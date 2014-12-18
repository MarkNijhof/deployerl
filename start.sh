#!/bin/bash

IP=`hostname -I | cut -f1 -d' '`

rm ./_rel/deployerl/releases/1/vm.args
echo "-setcookie will_be_overriden" >> ./_rel/deployerl/releases/1/vm.args
echo "-name `hostname`@$IP" >> ./_rel/deployerl/releases/1/vm.args

./_rel/deployerl/bin/deployerl start
