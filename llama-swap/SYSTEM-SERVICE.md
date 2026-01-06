# System Service Installation for llama-swap

This guide covers installing llama-swap as a system service that starts at boot and runs as your user.

## Prerequisites

- System running systemd
- User account (e.g., `merlin`)
- llama-swap binary built and available

## Installation Steps

### 1. Build llama-swap

```bash
cd /path/to/llama-swap
make build
```

### 2. Install binary to system location

```bash
sudo cp build/llama-swap-linux-amd64 /usr/local/bin/llama-swap
sudo chmod +x /usr/local/bin/llama-swap
```

### 3. Create service file

Create `/etc/systemd/system/llama-swap@.service` with the following content:

```ini
[Unit]
Description=llama-swap proxy server
Documentation=https://github.com/anomalyco/llama-swap
After=network.target

[Service]
User=%i
Group=%i
Type=simple
Restart=on-failure
RestartSec=5
WorkingDirectory=/home/%i/git/llama-swap/build
ExecStart=/usr/local/bin/llama-swap --config /home/%i/abrazo/llama-swap/config.yaml --listen 0.0.0.0:8080
ExecReload=/bin/kill -HUP $MAINPID

# Environment variables
Environment="TORCH_ROCM_AOTRITON_ENABLE_EXPERIMENTAL=1"
Environment="XDG_CACHE_HOME=/home/%i/.cache/"
Environment="LOGNAME=%i"

# Logging
StandardOutput=journal
StandardError=journal
SyslogIdentifier=llama-swap

# Resource limits
LimitNOFILE=65536
LimitNPROC=4096

# Minimal security
NoNewPrivileges=true

[Install]
WantedBy=default.target
```

### 4. Enable and start service

```bash
sudo systemctl daemon-reload
sudo systemctl enable llama-swap@merlin.service
sudo systemctl start llama-swap@merlin.service
```

Replace `merlin` with your actual username.

## Management Commands

```bash
# Check status
sudo systemctl status llama-swap@merlin.service

# View logs
sudo journalctl -u llama-swap@merlin.service -f

# Restart service
sudo systemctl restart llama-swap@merlin.service

# Stop service
sudo systemctl stop llama-swap@merlin.service

# Disable autostart
sudo systemctl disable llama-swap@merlin.service
```

## Troubleshooting

### Permission denied on executable
```bash
sudo chmod +x /usr/local/bin/llama-swap
```

### Check logs for errors
```bash
sudo journalctl -u llama-swap@merlin.service -n 50
```

### Verify binary and config exist
```bash
ls -la /usr/local/bin/llama-swap
ls -la /home/merlin/abrazo/llama-swap/config.yaml
```

### Test manually
```bash
/usr/local/bin/llama-swap --config /home/merlin/abrazo/llama-swap/config.yaml --listen 0.0.0.0:8080
```

## Configuration

- Service runs as your user (`merlin`)
- Listens on `0.0.0.0:8080`
- Uses config at `/home/merlin/abrazo/llama-swap/config.yaml`
- Starts automatically at boot

Modify the service file if you need different paths or ports, then reload with `sudo systemctl daemon-reload`.