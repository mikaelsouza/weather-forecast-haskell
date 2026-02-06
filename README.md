# JJ - Weather Forecast TUI

JJ is a minimalist, dependency-free Text User Interface (TUI) for 7-day weather forecasts, built entirely in Haskell.

## Features

- **Zero External Dependencies**: Implements everything from the ground up using only Haskell's `base` and system boot libraries.
- **Custom Networking**: Low-level TCP socket implementation via FFI to `libc` for DNS and data transport.
- **Scratch HTTP/1.1 Client**: Supports GET requests and Chunked Transfer Encoding (required for modern APIs).
- **Hand-rolled JSON Parser**: Recursive descent parser for weather data and geocoding results.
- **ANSI-powered TUI**: A beautiful, color-coded dashboard showing conditions and temperatures in both **Celsius and Fahrenheit**.
- **Comprehensive Testing**: Full test suite covering edge cases and fail states.

## Project Structure

- `src/Weather/`: Core library (Networking, HTTP, JSON, TUI logic).
- `app/`: Application entry point.
- `test/`: Unit tests and failure case verification.
- `Makefile`: Build automation.

## Getting Started

### Prerequisites

- GHC (Glasgow Haskell Compiler)

### Building

To compile the project:
```bash
make build
```

### Usage

Run the program with a city name:
```bash
./weather "New York"
```

### Testing

Run the automated test suite:
```bash
make test
```

## How it Works

JJ talks directly to the [Open-Meteo API](https://open-meteo.com/). It first resolves the city name to coordinates using the Geocoding API, then fetches the 7-day daily forecast. Since it uses no external Haskell libraries for HTTP or JSON, it implements:
1. **SocketTransport**: FFI bindings to POSIX `socket`, `connect`, etc.
2. **HttpClient**: HTTP/1.1 protocol handling including header parsing and chunked decoding.
3. **JsonParser**: A monadic parser combinator library built from scratch.

---
*Created with ❤️ in Haskell*
