-module(demo_SUITE).

-compile(export_all).

all() -> [ {group, demo} ].
groups() -> [ {demo, [], [ test_demo ]} ].
suite() -> [ {ct_hooks, [cth_surefire]}, {timetrap, 2000} ].

test_demo(Config) ->
    {_, _, X} = os:timestamp(),
    case X rem 5 of
        0 -> throw(minor_error);
        1 -> error(death_to_all);
        _ -> ok
    end.
