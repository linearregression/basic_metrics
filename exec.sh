#/bin/bbash
rm -rf ./log*
erl -setcookie lager -name basic_metrics@127.0.0.1 -pa ./_build/dev/lib/*/ebin -config ./config/sys.config -S basic_metrics_app
