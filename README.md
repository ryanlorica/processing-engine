# processing-engine

A Scala w/ Chisel based implementation of a processing engine generator for neural network accelerators.

## Introduction

The nPE is a highly configurable processing engine that can be topologically configured to support various DNN accelerator architectures. Its functionality is able to support inner products with varying amounts of spatial and temporal parallelism, nonlinear activation functions, scalar addition, scalar multiplication, and max functions. It supports many existing architectures and dataflows, such as those in Eyeriss, Flexflow, and ShiDianNao.
