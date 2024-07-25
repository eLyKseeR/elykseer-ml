# Tekton CI/CD definitions

This pipeline will build [eLyKseeR]() for architectures *amd64* and *arm64*
and push the resulting artefacts to a MinIO/S3 bucket.

Compilation will be done in the Docker image [codieplusplus/elykseer-ml](https://hub.docker.com/r/codieplusplus/elykseer-ml) (for the respective architecture)

## Prerequisites

- Kubernetes cluster and cli tools: `kubectl`
- installation of [Tekton pipelines](https://tekton.dev/docs/installation/)
- a [MinIO](https://min.io/) server

## Prepare tasks and pipeline

NS=cicd

kubectl apply -n ${NS} -f ca-cert.yaml
kubectl apply -n ${NS} -f minio-credentials.yaml
kubectl apply -n ${NS} -f task-lxr-cleanup.yaml
kubectl apply -n ${NS} -f task-lxr-compile-amd64.yaml
kubectl apply -n ${NS} -f task-lxr-compile-arm64.yaml
kubectl apply -n ${NS} -f pipeline.yaml


## Run pipeline

1. adapt `pipeline-run.yaml` with the commit hash

2. `kubectl create -n ${NS} -f pipeline-run.yaml`
