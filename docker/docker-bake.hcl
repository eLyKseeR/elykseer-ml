# Builds the base and the main image in one go; the main image uses the
# base image from this build (no need to push the base image first).
# run from within docker/:
#   BASE_VERSION=3 VERSION=v0.10.3 docker buildx bake --allow=fs.read=../.git main     # build only (stays in the build cache)
#   BASE_VERSION=3 VERSION=v0.10.3 docker buildx bake --allow=fs.read=../.git --push main  # build and push both

variable "BASE_VERSION" {
  default = "3"
}

variable "VERSION" {
  default = "dev"
}

variable "PLATFORMS" {
  default = "linux/amd64,linux/arm64"
}

group "default" {
  targets = ["base", "main"]
}

target "base" {
  context    = "."
  dockerfile = "Dockerfile.base"
  platforms  = split(",", PLATFORMS)
  tags       = ["codieplusplus/elykseer-ml:base_${BASE_VERSION}"]
}

target "main" {
  context    = "."
  dockerfile = "Dockerfile"
  platforms  = split(",", PLATFORMS)
  args = {
    BASE_VERSION = BASE_VERSION
  }
  contexts = {
    # the FROM in Dockerfile resolves to the base target of this build
    "codieplusplus/elykseer-ml:base_${BASE_VERSION}" = "target:base"
    # the committed state of the repository
    elykseer-ml-git = "../.git"
  }
  tags = ["codieplusplus/elykseer-ml:${VERSION}"]
}
