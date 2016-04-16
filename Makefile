PROJECT = terakol
PROJECT_DESCRIPTION = A REST Server atop Solr Engine
PROJECT_VERSION = 1.0.0
CONFIG ?= rel/sys.config

DEPS = cowboy lager jsx sync iso8601 uuid erlpass sync ibrowse
SHELL_OPTS = -s ${PROJECT} -config ${CONFIG}

dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.4
dep_lager = git https://github.com/basho/lager.git 3.1.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.0
dep_mongodb = git https://github.com/comtihon/mongodb-erlang.git v0.9.7
dep_iso8601 = git https://github.com/mhishami/erlang_iso8601.git 1.1.2
dep_erlpass = git https://github.com/mhishami/erlpass.git 1.0.2

include erlang.mk
