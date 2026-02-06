# JJ - Weather Forecast TUI 🌦️

JJ is a high-performance, minimalist Text User Interface (TUI) for 7-day weather forecasts. It is built entirely in **pure Haskell** with **zero external dependencies**, leveraging direct FFI bindings to the system's C libraries.

## ✨ Key Features

- **🚀 Zero Dependencies**: No `cabal` or `stack` needed. Just `ghc` and `make`.
- **🔌 Low-Level Networking**: Implements its own TCP socket layer via FFI to `libc`.
- **🛠️ Custom HTTP/1.1 Client**: Built from scratch to handle GET requests, headers, and Chunked Transfer Encoding.
- **🔡 Monadic JSON Parser**: A hand-rolled recursive descent parser for API responses.
- **🎨 Beautiful TUI**: Color-coded output using ANSI escape sequences with support for 50+ weather conditions.
- **🌡️ Dual Units**: Displays all temperatures in both **Celsius** and **Fahrenheit** simultaneously.
- **🧪 Robust Testing**: Includes a full suite of unit tests for the JSON and API logic.

## 🚀 Quick Start (Reproduce in 30 seconds)

### 1. Build the project
```bash
make build
```

### 2. Run the forecast
```bash
./weather "New York"
```

### 3. Version Control with Jujutsu (JJ)
This project is managed using `jj`. To see the current status:
```bash
jj status
```

### 4. Run the test suite
```bash
make test
```

## 📂 Project Structure

- `app/Main.hs`: Command-line interface and error handling.
- `src/Weather/`:
  - `SocketTransport.hs`: Low-level C bindings for networking.
  - `HttpClient.hs`: Custom HTTP implementation.
  - `JsonParser.hs`: Monadic JSON parsing engine.
  - `WeatherAPI.hs`: Open-Meteo integration and geocoding.
  - `TUI.hs`: ANSI rendering and weather condition logic.
- `test/Tests.hs`: Comprehensive unit tests.
- `Makefile`: Simple build system.

## 🛠️ Technical Implementation

JJ communicates directly with the **Open-Meteo API**.
1. **Geocoding**: Converts a city string into coordinates via the Search API.
2. **Forecast**: Fetches a 7-day daily forecast including weather codes and min/max temperatures.
3. **Rendering**: Standardizes output to a 65-character wide frame for consistent terminal display.

---
*Developed with focus on simplicity and performance in pure Haskell. This entire project, including the custom networking stack and JSON parser, was written by an AI (Antigravity).*
