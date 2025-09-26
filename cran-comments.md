## 0.2.1 Release summary
Corrected invalid URI for CODE_OF_CONDUCT.md following CRAN feedback on v0.2.0.
Added possible spelling error to allowed wordlist.

### R CMD check results
There were no ERRORs, or WARNINGs.  
1 NOTE = maintainer change.

The remaining comments for v0.2.0 below are still relevant.

## 0.2.0 Release summary
This is second release for the NHS-R Community's 'Plot the Dots' package.  It adds a few new features and fixes some minor bugs. It resolves some build failures that are currently present, due to upstream dependencies (ggplot2 4.0.0).

There has been a change of maintainer from Christopher Reading to Tom Smith.

### Test environments
* local windows 11, R 4.5.1

* GitHub actions:
  * macOS 15.5 24F74, R 4.5.1 
  * Windows Server 2022 10.0.20348, R 4.5.1
  * Ubuntu 24.04.3 LTS, R 4.5.1
  * Ubuntu 24.04.3 LTS, R-devel 2025-09-11 r88813
  * Ubuntu 24.04.3 LTS, R 4.4.3

* r-hub:
  * Ubuntu 24.04.3 LTS, R-devel 2025-09-23 r88869
  * macOS 15.6.1 24G90 (arm64), R-devel 2025-09-24 r88873
  * macOS 13.7.6 22H625 (x86), R-devel 2025-09-24 r88873
  * Windows Server 2025 10.0.26100, R-devel 2025-09-24 r88873 ucrt

### R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

### Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
