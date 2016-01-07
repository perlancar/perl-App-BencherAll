package App::BencherAll;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Bencher;
use File::Slurper qw(write_text);
use Function::Fallback::CoreOrPP qw(clone);
use JSON qw(encode_json);
use Perinci::Sub::Util qw(err);
use PERLANCAR::Module::List;
use POSIX qw(strftime);

our %SPEC;

$SPEC{bencher_all} = {
    v => 1.1,
    summary => 'Find all installed Bencher scenarios, run them, log results',
    description => <<'_',

This script provides a convenience way to run all bencher scenarios and store
the results as JSON files in a directory, for archival purpose.

By default, bencher results are stored as JSON in the `log_dir` directory, with
the following name pattern:

    <NAME>-<SUBNAME>.<yyyy>-<mm>-<dd>T<HH>-<MM>-<SS>.json
    <NAME>-<SUBNAME>.module_startup.<yyyy>-<mm>-<dd>T<HH>-<MM>-<SS>.json

_
    args => {
        log_dir => {
            schema => 'str*',
            req => 1,
        },
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
        },
        includes => {
            'x.name.is_plural' => 1,
            summary => 'Scenarios to include',
            schema => ['array*', of=>'str*'],
            tags => ['category:filtering'],
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
    },
    features => {
        dry_run => 1,
    },
};
sub bencher_all {
    my %args = @_;

    # find all installed scenarios
    my @scenarios;
    {
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
        my $filtered;
        if ($args{includes} && @{ $args{includes} }) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next unless grep {$s eq $_} @{ $args{includes} };
                push @fscenarios, $s;
            }
            @scenarios = @fscenarios;
        }
        if ($args{excludes} && @{ $args{excludes} }) {
            $filtered++;
            my @fscenarios;
            for my $s (@scenarios) {
                next if grep {$s eq $_} @{ $args{excludes} };
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
            my $res;

            my $timestamp = strftime("%Y-%m-%dT%H-%M-%S", localtime);
            my $sn_encoded = $sn; $sn_encoded =~ s/::/-/g;

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

            $res = Bencher::bencher(
                action => 'bench',
                scenario_module => $sn);
            return err("Can't bench", $res) unless $res->[0] == 200;
            my $filename = "$args{log_dir}/$sn_encoded.$timestamp.json";
            $log->tracef("Writing file %s ...", $filename);
            write_text($filename, encode_json($res));

            if (!$scenario->{module_startup} && @$modules) {
                $res = Bencher::bencher(
                    action => 'bench',
                    module_startup => 1,
                    scenario_module => $sn);
                return err("Can't bench (module_startup)", $res)
                    unless $res->[0] == 200;
                my $filename = "$args{log_dir}/$sn_encoded.module_startup.".
                    "$timestamp.json",
                $log->tracef("Writing file %s ...", $filename);
                write_text($filename, encode_json($res));
            }
        } # for scenario
    }

    [200];
}

$SPEC{bencher_all_under_lib} = do {
    my $meta = $SPEC{bencher_all};
    delete $meta->{args}{no_lib};
    delete $meta->{args}{libs};
    $meta->{summary} = "Shortcut for bencher-all --no-lib --lib lib";
    delete $meta->{description};
    $meta;
};
sub bencher_all_under_lib {
    bencher_all(@_, no_lib=>1, libs=>["lib"]);
}

1;
# ABSTRACT:

=head1 SYNOPSIS

See L<bencher-all> and L<bencher-all-under-lib>.
