# stop-watch
Informational ticker for an MBTA stop


**Requirements**
---
Go 1.19 or later

## Installation

### Using Go
```bash
go install github.com/nstapc/stop-watch@latest
```

### Or build from source:
```bash
git clone https://github.com/nstapc/stop-watch.git
cd stop-watch
go build -o stop-watch main.go
```

## Usage

### Basic Usage
```bash
./stop-watch <stop-id>
```

Example:
```bash
./stop-watch place-pktrm
```

This shows real-time predictions for Park Street station.

## Finding Stop IDs

Find stop IDs at https://www.mbta.com/ or use the MBTA API:

```bash
curl "https://api-v3.mbta.com/stops?page%5Blimit%5D=10" | jq '.data[] | "\(.id): \(.attributes.name)"'
```
