REBAR=./rebar
FLEXPATH=~/flex

.PHONY:deps flex

all: deps compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) check-deps || $(REBAR) get-deps

flex: priv/flex/view.mxml priv/flex/broadcast.mxml 
	$(FLEXPATH)/bin/mxmlc priv/flex/view.mxml -sp ./ -o priv/www/swf/view.swf -use-network=false -include-libraries priv/flex/libs/OSMF.swc -static-link-runtime-shared-libraries=true
	$(FLEXPATH)/bin/mxmlc priv/flex/broadcast.mxml -sp ./ -o priv/www/swf/broadcast.swf -use-network=false -static-link-runtime-shared-libraries=true

