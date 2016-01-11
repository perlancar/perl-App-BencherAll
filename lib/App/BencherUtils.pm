package App::BencherUtils;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Function::Fallback::CoreOrPP qw(clone);
use Perinci::Sub::Util qw(err);
use POSIX qw(strftime);

our %SPEC;

$SPEC{':package'} = {
    v => 1.1,
    summary => 'Utilities related to bencher',
};

my %args_common = (
    results_dir => {
        summary => 'Directory to store results files in',
        schema => 'str*',
        req => 1,
    },
);

my %args_common_query = (
    query => {
        schema => ['array*', of=>'str*'],
        pos => 0,
        greedy => 1,
    },
    detail => {
        schema => 'bool',
        cmdline_aliases => {l=>{}},
    },
);

sub _json {
    state $json = do {
        require JSON;
        JSON->new;
    };
    $json;
}

sub _complete_scenario_module {
    require Complete::Module;
    my %args = @_;
    Complete::Module::complete_module(
        word=>$args{word}, ns_prefix=>'Bencher::Scenario');
}

$SPEC{bencher_all} = {
    v => 1.1,
    summary => 'Find all installed Bencher scenarios, run them, log results',
    description => <<'_',

This script provides a convenience way to run all bencher scenarios and store
the results as JSON files in a directory, for archival purpose.

By default, bencher results are stored as JSON in the `results_dir` directory,
with the following name pattern:

    <NAME>-<SUBNAME>.<yyyy>-<mm>-<dd>T<HH>-<MM>-<SS>.json
    <NAME>-<SUBNAME>.module_startup.<yyyy>-<mm>-<dd>T<HH>-<MM>-<SS>.json

_
    args => {
        %args_common,
        no_lib => {
            summary => 'Remove entries from @INC',
            schema => ['bool*', is=>1],
        },
        libs => {
            'x.name.is_plural' => 1,
            summary => 'Add to the start of @INC',
            schema => ['array*', of=>'str*'],
            cmdline_aliases => {
                I => {code=>sub { $_[0]{libs} //= []; push @{ $_[0]{libs} }, $_[1] }},
            },
        },

        excludes => {
            'x.name.is_plural' => 1,
            summary => 'Scenarios to exclude',
            schema => ['array*', of=>'str*'],
            tags => ['category:filtering'],
            element_completion => \&_complete_scenario_module,
        },
        includes => {
            'x.name.is_plural' => 1,
            summary => 'Scenarios to include',
            schema => ['array*', of=>'str*'],
            tags => ['category:filtering'],
            element_completion => \&_complete_scenario_module,
        },
        exclude_patterns => {
            'x.name.is_plural' => 1,
            summary => 'Scenario patterns to exclude',
            schema => ['array*', of=>'re*'],
            tags => ['category:filtering'],
        },
        include_patterns => {
            'x.name.is_plural' => 1,
            summary => 'Scenario patterns to include',
            schema => ['array*', of=>'re*'],
            tags => ['category:filtering'],
        },

        precision_limit => {
            summary => 'Precision limit, passed to bencher',
            schema => ['float*', between=>[0,1]],
            tags => ['category:bencher'],
        },
    },
    features => {
        dry_run => 1,
    },
};
sub bencher_all {
    require Bencher;
    require File::Slurper;

    my %args = @_;

    # find all installed scenarios
    my @scenarios;
    {
        require PERLANCAR::Module::List;

        local @INC = @INC;
        @INC = () if $args{no_lib};
        unshift @INC, reverse(@{$args{libs}}) if $args{libs};
        $log->tracef("\@INC is %s", \@INC);
        my $res = PERLANCAR::Module::List::list_modules(
            'Bencher::Scenario::', {list_modules=>1, recurse=>1});
        @scenarios = map {s/\ABencher::Scenario:://; $_} sort keys %$res;
        $log->tracef("Scenario modules found: %s", \@scenarios);
        return [304, "No scenario modules found under lib/"] unless @scenarios;
    }

  FILTER:
    {
        # normalize
        my $includes = [
            map {s!/!::!g; $_} @{ $args{includes} // [] }
        ];
        my $excludes = [
            map {s!/!::!g; $_} @{ $args{excludes} // [] }
        ];

        my $filtered;
        if (@$includes) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next unless grep {$s eq $_} @$includes;
                push @fscenarios, $s;
            }
            @scenarios = @fscenarios;
        }
        if (@$excludes) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next if grep {$s eq $_} @$excludes;
                push @fscenarios, $s;
            }
            @scenarios = @fscenarios;
        }
        if ($args{include_patterns} && @{ $args{include_patterns} }) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next unless grep {ref($_) eq 'Regexp' ? $s =~ $_ : $s =~ /$_/ }
                    @{ $args{include_patterns} };
                push @fscenarios, $s;
            }
            @scenarios = @fscenarios;
        }
        if ($args{exclude_patterns} && @{ $args{exclude_patterns} }) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next if grep {ref($_) eq 'Regexp' ? $s =~ $_ : $s =~ /$_/ }
                    @{ $args{exclude_patterns} };
                push @fscenarios, $s;
            }
            @scenarios = @fscenarios;
        }

        if ($filtered) {
            $log->tracef("Scenario modules after filtering: %s", \@scenarios);
        }
        return [304, "No scenario modules to run"] unless @scenarios;
    }

    return [304, "Dry-run"] if $args{-dry_run};

    # bencher'em all!
    {
        local @INC = ("lib", @INC);
        for my $sn (@scenarios) {
            $log->infof("Processing scenario %s ...", $sn);
            eval {
                my $res;

                my $timestamp = strftime("%Y-%m-%dT%H-%M-%S", localtime);
                my $sn_encoded = $sn; $sn_encoded =~ s/::/-/g;

                $res = Bencher::bencher(
                    action => 'bench',
                    scenario_module => $sn,
                    precision_limit => $args{precision_limit},
                );
                return err("Can't bench", $res) unless $res->[0] == 200;
                my $filename = "$args{results_dir}/$sn_encoded.$timestamp.json";
                $log->tracef("Writing file %s ...", $filename);
                File::Slurper::write_text($filename, _json->encode($res));

                $res = Bencher::bencher(
                    action => 'show-scenario',
                    scenario_module => $sn);
                return err("Can't show scenario", $res) unless $res->[0] == 200;
                my $scenario = $res->[2];

                $res = Bencher::bencher(
                    action => 'list-participant-modules',
                    scenario_module => $sn);
                return err("Can't list participant modules", $res)
                    unless $res->[0] == 200;
                my $modules = $res->[2];

                if (!$scenario->{module_startup} && @$modules) {
                    $res = Bencher::bencher(
                        action => 'bench',
                        module_startup => 1,
                        scenario_module => $sn);
                    return err("Can't bench (module_startup)", $res)
                        unless $res->[0] == 200;
                    my $filename = "$args{results_dir}/$sn_encoded.module_startup.".
                        "$timestamp.json",
                        $log->tracef("Writing file %s ...", $filename);
                    File::Slurper::write_text($filename, _json->encode($res));
                }
            }; # eval

            if ($@) {
                $log->error("Dies (%s), skipping to the next scenario", $@);
            }
        } # for scenario
    }

    [200];
}

$SPEC{bencher_all_under_lib} = do {
    my $meta = clone($SPEC{bencher_all});
    delete $meta->{args}{no_lib};
    delete $meta->{args}{libs};
    $meta->{summary} = "Shortcut for bencher-all --no-lib --lib lib";
    delete $meta->{description};
    $meta;
};
sub bencher_all_under_lib {
    bencher_all(@_, no_lib=>1, libs=>["lib"]);
}

my $re_filename = qr/\A
                     (\w+(?:-(?:\w+))*)
                     (\.module_startup)?
                     \.(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d)-(\d\d)-(\d\d)
                     \.json
                     \z/x;

sub _complete_scenario_in_results_dir {
    require Complete::Util;

    my %args = @_;
    my $word = $args{word};
    my $cmdline = $args{cmdline};
    my $r = $args{r};

    return undef unless $cmdline;

    $r->{read_config} = 1;

    my $res = $cmdline->parse_argv($r);
    return undef unless $res->[0] == 200;

    # combine from command-line and from config/env
    my $final_args = { %{$res->[2]}, %{$args{args}} };
    #$log->tracef("final args=%s", $final_args);

    return [] unless $final_args->{results_dir};

    my %scenarios;

    opendir my($dh), $final_args->{results_dir} or return undef;
    for my $filename (readdir $dh) {
        $filename =~ $re_filename or next;
        my $sc = $1; $sc =~ s/-/::/g;
        $scenarios{$sc}++;
    }

    Complete::Util::complete_hash_key(hash=>\%scenarios, word=>$word);
}

$SPEC{list_bencher_results} = {
    v => 1.1,
    summary => "List results in results directory",
    args => {
        %args_common,
        %args_common_query,

        include_scenarios => {
            'x.name.is_plural' => 1,
            schema => ['array*', of=>'str*'],
            tags => ['category:filtering'],
            element_completion => \&_complete_scenario_in_results_dir,
        },
        exclude_scenarios => {
            'x.name.is_plural' => 1,
            schema => ['array*', of=>'str*'],
            tags => ['category:filtering'],
            element_completion => \&_complete_scenario_in_results_dir,
        },
        module_startup => {
            schema => 'bool*',
            tags => ['category:filtering'],
        },
        latest => {
            'summary.alt.bool.yes' => 'Only list the latest result for every scenario+CPU',
            'summary.alt.bool.no'  => 'Do not list the latest result for every scenario+CPU',
            schema => ['bool*'],
            tags => ['category:filtering'],
        },

        fmt => {
            summary => 'Display each result with bencher-fmt',
            schema => 'bool*',
            tags => ['category:display'],
        },
    },
    examples => [
        {
            summary => 'List all results',
            src => 'list-bencher-results',
            src_plang => 'bash',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            summary => 'List all results, show detail information',
            src => 'list-bencher-results -l',
            src_plang => 'bash',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            summary => 'List matching results only',
            src => 'list-bencher-results --exclude-scenario Perl::Startup Startup',
            src_plang => 'bash',
            test => 0,
            'x.doc.show_result' => 0,
        },
        {
            summary => 'List matching results, format all using bencher-fmt',
            src => 'list-bencher-results --exclude-scenario Perl::Startup Startup --fmt',
            src_plang => 'bash',
            test => 0,
            'x.doc.show_result' => 0,
        },
    ],
};
sub list_bencher_results {
    require File::Slurper;

    my %args = @_;

    my $dir = $args{results_dir};

    opendir my($dh), $dir or return [500, "Can't read results_dir `$dir`: $!"];

    # normalize
    my $include_scenarios = [
        map {s!/!::!g; $_} @{ $args{include_scenarios} // [] }
    ];
    my $exclude_scenarios = [
        map {s!/!::!g; $_} @{ $args{exclude_scenarios} // [] }
    ];

    my %latest; # key = module+(module_startup)+cpu, value = latest row
    my @rows;
  FILE:
    for my $filename (sort readdir $dh) {
        next unless $filename =~ $re_filename;
        my $row = {
            filename => $filename,
            scenario => $1,
            module_startup => $2 ? 1:0,
            time => "$3-$4-$5T$6:$7:$8",
        };
        $row->{scenario} =~ s/-/::/g;
        if (@$include_scenarios) {
            next FILE unless grep {$row->{scenario} eq $_} @$include_scenarios;
        }
        if (@$exclude_scenarios) {
            next FILE if grep {$row->{scenario} eq $_} @$exclude_scenarios;
        }

        my $benchres = _json->decode(File::Slurper::read_text("$dir/$filename"));
        $row->{res} = $benchres;
        $row->{cpu} = $benchres->[3]{'func.cpu_info'}[0]{name};

        $row->{module_startup} = 1 if $benchres->[3]{'func.module_startup'};

        if (defined $args{module_startup}) {
            next FILE if $row->{module_startup} xor $args{module_startup};
        }

        my $key = sprintf(
            "%s.%s.%s",
            $row->{scenario},
            $row->{module_startup} ? 0:1,
            $row->{cpu},
        );

        if ($args{query} && @{ $args{query} }) {
            my $matches = 1;
          QUERY_WORD:
            for my $q (@{ $args{query} }) {
                my $lq = lc($q);
                if (index(lc($row->{cpu}), $lq) == -1 &&
                        index(lc($row->{filename}), $lq) == -1 &&
                        index(lc($row->{scenario}), $lq) == -1
                ) {
                    $matches = 0;
                    last;
                }
            }
            next unless $matches;
        }

        if (!$latest{$key} || $latest{$key}{time} lt $row->{time}) {
            $latest{$key} = $row;
        }
        $row->{_key} = $key;

        push @rows, $row;
    }

    # we do the 'latest' filter here after we get all the rows
    if (defined $args{latest}) {
        my @rows0 = @rows;
        @rows = grep {
            my $latest_time = $latest{ $_->{_key} }{time};
            $args{latest} ?
                $_->{time} eq $latest_time :
                $_->{time} ne $latest_time;
        } @rows;
    }

    for (@rows) { delete $_->{_key} }

    my $resmeta = {};
    if ($args{fmt}) {
        require Bencher;
        require Perinci::Result::Format::Lite;

        $resmeta->{'cmdline.skip_format'} = 1;
        my @content;
        for my $row (@rows) {
            push @content, "$row->{filename} (cpu: $row->{cpu}):\n";
            my $res = Bencher::format_result($row->{res});
            push @content, Perinci::Result::Format::Lite::format(
                $res, "text-pretty");
            push @content, "\n";
        }
        return [200, "OK", join("", @content), $resmeta];
    } else {
        delete($_->{res}) for @rows;
        if ($args{detail}) {
            $resmeta->{'table.fields'} = [qw(scenario module_startup time cpu filename)];
        } else {
            @rows = map {$_->{filename}} @rows;
        }
        return [200, "OK", \@rows, $resmeta];
    }
}

1;
# ABSTRACT:

=head1 SYNOPSIS

=head1 DESCRIPTION

This distribution includes several utilities:

#INSERT_EXECS_LIST
