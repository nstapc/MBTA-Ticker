# MBTA-Ticker
Informational ticker for an MBTA stop


**Requirements**
---
Go 1.19 or later

## Installation

### Using Go
```bash
go install github.com/nstapczynski/MBTA-Ticker@latest
```

### Or build from source:
```bash
git clone https://github.com/nstapczynski/MBTA-Ticker.git
cd MBTA-Ticker
go build -o mbta-ticker main.go
```

## Usage

### Basic Usage
```bash
./mbta-ticker <stop-id>
```

Example:
```bash
./mbta-ticker place-pktrm
```

This shows real-time predictions for Park Street station.

## Finding Stop IDs

Find stop IDs at https://www.mbta.com/ or use the MBTA API:

```bash
curl "https://api-v3.mbta.com/stops?page[limit]=10" | jq '.data[] | "\(.id): \(.attributes.name)"'
```
