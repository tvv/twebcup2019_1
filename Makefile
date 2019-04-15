APPNAME=wa
SERVICENAME=twebcup2019_1

REBAR=./rebar3
ERL=erl
ERLC=erlc

PLTFILE=$(CURDIR)/.deps.plt

PATH_TO_CONFIG=config/$(shell hostname).sys.config
ifneq ("$(wildcard $(PATH_TO_CONFIG))","")
	CONFIG=$(PATH_TO_CONFIG)
else
	CONFIG=config/sys.config
endif

all: compile

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3 -O $(REBAR)
	chmod +x $(REBAR)

compile: $(REBAR)
	@$(REBAR) compile

dialyzer: $(REBAR)
	@$(REBAR) dialyzer

clean: $(REBAR)
	@$(REBAR) clean

release: $(REBAR)
	@$(REBAR) as ${profile} release

release-tar: $(REBAR)
	@$(REBAR) as ${profile} tar

upgrade: $(REBAR)
	@$(REBAR) upgrade

tests: ct

ct: $(REBAR)
	@$(REBAR) ct --sys_config config/sys.config

run: $(REBAR)
	epmd -daemon
	@echo "Hostname is $(shell hostname)"
	@echo "Config ${CONFIG} used"
	@$(REBAR) shell +pc unicode --config $(CONFIG) --sname $(SERVICENAME)_$(APPNAME)@localhost

gen_models: compile
	ERL_LIBS=_build/default/lib/ escript bin/model ${models}

frontend-build:
	cd priv/frontend && yarn run build

frontend-watch:
	cd priv/frontend && yarn run watch
	