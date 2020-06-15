-define(HAMLER_INSTALL_DIR, "/usr/local/lib/hamler").

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[reba3 hamler] " ++ FORMAT, ARGS)).