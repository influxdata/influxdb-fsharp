#!/usr/bin/env bash

echo "Provisioning virtual machine..."

echo "Download InfluxDB deb package..."
wget -q https://s3.amazonaws.com/influxdb/influxdb_0.9.1_amd64.deb

echo "Install InfluxDB..."
sudo dpkg -i influxdb_0.9.1_amd64.deb

echo "Start InfluxDB daemon..."
sudo /etc/init.d/influxdb start

echo "Setting chmod to provision files..."
chmod +x dropalldb.sh
