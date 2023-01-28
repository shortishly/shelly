#-*- mode: makefile-gmake -*-
# Copyright (c) 2012-2022 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = shelly
PROJECT_DESCRIPTION = secure shell

DEPS = \
	envy

RELX_TAR = 0
COVER = 1

LOCAL_DEPS = \
	crypto \
	sasl \
	ssh

dep_envy = git https://github.com/shortishly/envy.git
dep_envy_commit = 0.7.2


SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync


SHELL_DEPS = \
	sync


BUILD_DEPS += relx
include erlang.mk

priv/ssh/system/ssh_host_rsa_key.pub:
	$(gen_verbose) ssh-keygen -N "" -t rsa -f priv/ssh/system/ssh_host_rsa_key

distclean-host-key:
	$(gen_verbose) rm -f priv/ssh/system/ssh_host_rsa_key priv/ssh/system/ssh_host_rsa_key.pub

app:: priv/ssh/system/ssh_host_rsa_key.pub

distclean:: distclean-host-key
