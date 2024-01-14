[Content](01_Content.md)

# Modules

The code is organised in modules which can further be grouped:


## Core modules

### Configuration
[Configuration](../theories/Configuration.v)

Defines program configuration.

### Nchunks
[Nchunks](../theories/Nchunks.v)

Provides a special type *Nchunks* to restrict it to the interval `[1,256]`

### Buffer
[Buffer](../theories/Buffer.v)

An abstraction of a memory blob.

### Environment
[Environment](../theories/Environment.v)

This module provides high-level API.

### Assembly
[Assembly](../theories/Assembly.v)

This module implements the functionality of an assembly.


## Functionality modules

### BackupPlanner
[BackupPlanner](../theories/BackupPlanner.v)

Computes a plan to backup a file.

### AssemblyCache
[AssemblyCache](../theories/AssemblyCache.v)

Provides a cache of assemblies and recreates them on demand.


## Other modules

### Conversion
[Conversion](../theories/Conversion.v)

various conversion primitives.

### Filesupport
[Filesupport](../theories/Filesupport.v)

This module provides types on file information.

### Utilities
[Utilities](../theories/Utilities.v)


### MakeML
[MakeML](../theories/MakeML.v)

This module defines the code export to *OCaml*.

### Version
[Version](../theories/Version.v)

This module defines the current version.