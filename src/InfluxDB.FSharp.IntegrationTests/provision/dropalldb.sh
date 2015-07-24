#!/usr/bin/env bash

IFS=$'\n'
arr=$(/opt/influxdb/influx -execute 'SHOW DATABASES' | tail -n+4)
for db in ${arr[@]};
    do /opt/influxdb/influx -execute "DROP DATABASE $db";
done
