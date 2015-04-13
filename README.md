##ct_refresh

Common Test refresh plugin, registered as event handler in `rebar.config`. Make sure it's on the path, eg. as a dependency:
```
{ct_extra_params, "-event_handler ct_refresh_events"}.

{deps, [
    {ct_refresh, ".*", {git, "https://github.com/konrads/ct_refresh.git", {branch, "master"}}}
]}.
```

Upon test run finish (triggered on second write of "**/all_runs.html"):

1. re-links dirs to have a "latest" link for each suite
2. copies `index.html` -> `latest_index.html`, with new links in place
3. if env var CT_REFRESH_CHROME_START=true, and debug chrome is started (with `--remote-debugging-port`), refresh tabs that smell like CT test reports

### Utilized env vars:
* CT_REFRESH_CHROME_PORT - chrome debug port to (specified with `--remote-debugging-port`)
* CT_REFRESH_CHROME_EXEC - defaults to `/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome`
* CT_REFRESH_CHROME_START - `true` | `false`
