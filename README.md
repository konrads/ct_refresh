## ct_refresh

CommonTest plugin that a) links up latest test reports with `latest_*` links, b) optionally refreshes `latest_*` reports shown in chrome (with remote debugging), c) opionally starts chrome (with remote debugging) and points it at root `latest_index.html`.


#### Sample test run
```
CT_REFRESH_CHROME_START=true rebar -C demo_rebar.config ct
```


#### Installation/configuration:
Add to your rebar.config:
```
{ct_extra_params, "-event_handler ct_refresh_events"}.

{deps, [
    {ct_refresh, ".*", {git, "https://github.com/konrads/ct_refresh.git", {branch, "master"}}}
]}.
```

By default, browser interaction is switched off to avoid messing with CI. For browser instantiation/refreshing, configure:
* CT_REFRESH_CHROME_START - `true` | `false`
* CT_REFRESH_CHROME_PORT - chrome dev tools debug port, as specified with `--remote-debugging-port`. Defaults to 922
* CT_REFRESH_CHROME_EXEC - defaults to `/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome` on Darwin, "google-chrome" on Linux


#### Snags:
* Unsure about where this should be plugged in.  Tried ct hooks, ct halt functions, rebar plugins, but none of these would guarantee being run regardless of failure/success.
* As an event_handler, the access to stdout is limited, hence all output goes to `raw.log`
