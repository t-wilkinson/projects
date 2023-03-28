#!/bin/sh
# values2=$($rcli --csv mget $($rcli --csv 'keys' '*' | tr ',"' '  ')) # possibly buggy

echo $(pwd)
rcli="redis-6.0.9/src/redis-cli"
values1=$(pr -TmJS":" <($rcli 'keys' '*' | xargs -I '{}' -d '\n' -n 1 -P8 echo '"{}"') <($rcli 'keys' '*' | xargs -d '\n' -n 1 -P8 $rcli get | xargs -d '\n' -n 1 -I {} echo '{},' | tr '"' -d ))
echo {$values1 | sed -e 's/,$/}/' >| store-titles.json
brotli -f store-titles.json -q 11
