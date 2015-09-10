#!/usr/bin/env bash

echo "Provisioning virtual machine..."

influxpkg="influxdb_0.9.3_amd64.deb"

echo "Download InfluxDB deb package ${influxpkg}..."
wget -q "https://s3.amazonaws.com/influxdb/${influxpkg}"

echo "Install InfluxDB..."
sudo dpkg -i ${influxpkg}

echo "Start InfluxDB daemon..."
sudo /etc/init.d/influxdb start

echo "Setting chmod to provision files..."
chmod +x dropalldb.sh
