# ChessComTournamentManagement

## Overview

ChessComTournamentManagement is a convenient package that will filter out players in a daily tournament who have become ineligible to participate post-registration. Avoid checking each player's profile individually and let the work be done for you. It's easy to use and only requires the tournament ID, maximum timeout percentage set for the tournament, and maximum time per move set for the tournament.

#### Details:

Provide the following information:
  * Tournament ID
  * Maximum timeout percentage in the past 90 days
  * Maximum allowed time per move in the past 90 days

Ineligible players are returned with the following data:
  * Username
  * Timeout percentage in the past 90 days
  * Time per move in the past 90 days

## How to install

#### Option 1: Download the release

1. Download the latest package release
2. Download the latest Rtools stable release version here if not previously installed: https://cran.r-project.org/bin/windows/Rtools/
3. In the terminal, run `R CMD INSTALL -build -I [path to downloaded asset]`
4. From the R console, run `install.packages("[path to downloaded asset]", repors=NULL, type="source")`

#### Option 2: Install from GitHub

1. Download the latest Rtools stable release version here if not previously installed: https://cran.r-project.org/bin/windows/Rtools/
2. Install the package `devtools` from CRAN: `install.packages('devtools')`
3. Run the following command from the R console: `devtools::install_github('mhelke/ChessComTournamentManagement')`

## Author

Matthew Helke

* Contact: [matthewhelke52@gmail.com](mailto:matthewhelke52@gmail.com)
* Github: [mhelke](https://github.com/mhelke)
* LinkedIn: [matthew-helke](https://www.linkedin.com/in/matthew-helke)

## Contributing

Issues, and feature requests are welcome!
Please add your request to the [issues page](https://github.com/mhelke/ChessComTournamentManagement/issues)

## License

Copyright (c) 2023 Matthew Helke

This project is [MIT licensed](https://github.com/mhelke/ChessComTournamentManagement/blob/master/LICENSE.md)
