PROJECT = ymixer
PROJECT_DESCRIPTION = Rest interface for Ymaha C5 mixers
PROJECT_VERSION = 0.0.1

DEPS = cowboy jiffy
dep_cowboy_commit = 2.2.0

DEP_PLUGINS = cowboy reload_mk
BUILD_DEPS = reload_mk

EUNIT_OPTS = verbose

include erlang.mk
