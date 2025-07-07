#! /bin/bash

python src/python/main.py \
    -c=pipe \
    -i=pipes/s2p-0 \
    -o=pipes/p2s-0 \
    -a=dqn \
    --clip=2 \
    --ddqn=true \
    --batch-size=64 \
    --lr=0.0005 \
    --device=cuda:2 \
    --memory-size=100_000 \
    --save-to=saved_dqn_csp_200 \
    --keepalive