---
layout: home
title: Installation
parent: Run Xoptfoil2
nav_order: 1
---


## Installation 
{: .no_toc }

The current version of Xoptfoil2 is available in the [Releases section](https://github.com/jxjo/Xoptfoil2/releases) of the GitHub repository. The assets include zip files for:
- a ready-built version for Windows
- the source files for building Xoptfoil2 under Linux

<span>Windows</span>{: .label .label-blue }

Download the Windows zip file and extract it into any directory, for example directly on the Desktop for a first try. Xoptfoil2 is a lightweight installation and does not install other artifacts on your PC.

To use Xoptfoil2 from any directory, add its installation folder to the Windows `Path` environment variable. This avoids having to copy `xoptfoil2.exe` into each new project folder.

<span>Linux</span>{: .label .label-red } (Debian-based)

Download the `Source Code` tar file and extract it in any folder. In addition to the standard development tools of a typical Linux distribution, the Fortran compiler and CMake are required. These tools can be installed with:

```
sudo apt install gfortran
sudo apt install cmake
```

In the project root, run the `build_linux.sh` script:

```
bash build_linux.sh
```

This builds and installs Xoptfoil2 to the repository-local default location `linux/bin`.

By copying `xoptfoil2` and `worker` to `/usr/local/bin`, they can be accessed from every directory. In some cases, you may need to mark the files as executable:

```
sudo chmod +x xoptfoil2
sudo chmod +x worker
```

If you prefer a system-wide installation via CMake, configure with `/usr/local` as install prefix and run install with `sudo`:

```
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=/usr/local
sudo cmake --build build --target install
```

Note: CMake appends `bin` because the project installs executables with `DESTINATION bin`, so the final location becomes `/usr/local/bin`.

Linux command names are case-sensitive. Use `xoptfoil2` (lowercase), not `Xoptfoil2`.

### CMake build

Independent of the helper scripts, the standard CMake workflow can be used on both platforms:

```
cmake -S . -B build
cmake --build build
cmake --build build --target install
```

To override the default install location:

```
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=<your_path>
```

The binary version string is defined centrally in the root `VERSION` file. For ad-hoc builds it can be overridden during configuration with `-DXOPTFOIL_VERSION_STRING=<value>`.

The default install locations are `windows/bin` (Windows) and `linux/bin` (Linux).


### Docker

Thanks to @jblamyatiffremerformer for sharing this Docker setup example.

It can be convenient to run Xoptfoil2 inside a Docker container.

The following Dockerfile builds the project and prepares a working directory for it:

```dockerfile
FROM ubuntu:24.04

LABEL maintainer="JB Lamy"
LABEL description="Dockerfile to build a container for Xoptfoil2 - Airfoil optimization tool"

# Avoid interaction during the installation process
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies (gfortran, cmake, make, git)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    gfortran \
    cmake \
    make \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Clone the repository locally
RUN git clone https://github.com/jxjo/Xoptfoil2.git /opt/xoptfoil2

# Set the project directory
WORKDIR /opt/xoptfoil2

# Build the program
RUN bash build_linux.sh

# Create a workspace for inputs and outputs
RUN mkdir -p /workspace
WORKDIR /workspace

# Add the binaries to the Docker PATH
ENV PATH="/opt/xoptfoil2/linux/bin:${PATH}"

# Start an interactive shell inside the container
CMD ["/bin/bash"]
```

#### How to use a Xoptfoil2 Docker image

This Docker image lets you run **Xoptfoil2** without installing any compilers or dependencies on your machine. Everything needed for the build and execution is packaged in the container.

##### Step 1 — Install Docker

Docker is a tool that runs isolated environments called **containers**. Think of it as a lightweight virtual machine that includes only what is necessary.

*On Linux (Ubuntu / Debian)*

Open a terminal and run:

```bash
sudo apt-get update
sudo apt-get install -y docker.io
```

##### Step 2 — Add your user to the Docker group

Add your user to the Docker group so you do not need to use sudo every time:

```bash
sudo usermod -aG docker $USER
```

##### Step 3 — Verify the installation

Log out and log back in for the change to take effect. Then verify the installation:

```bash
docker --version
```

You should see output similar to Docker version 24.x.x.

*On Windows / macOS*

Download Docker Desktop from https://www.docker.com/products/docker-desktop and follow the installer. Once installed, open a terminal (PowerShell on Windows or Terminal on macOS) and verify:

```bash
docker --version
```

##### Step 4 — Build the image

From the project root, run:

```bash
docker build -t xoptfoil2 .
```

##### Step 5 — Run the container

Start an interactive shell:

*On Linux/macOS:*

```bash
docker run -it --rm -v "$(pwd)":/workspace xoptfoil2
```

*On Windows PowerShell:*

```powershell
docker run -it --rm -v "${PWD}:/workspace" xoptfoil2
```

From inside the container, you can run Xoptfoil2 commands, for example:

```bash
xoptfoil2 --help
```