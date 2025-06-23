#! /bin/bash

python src/python/main.py \
    -c=pipe \
    -i=pipes/s2p-0 \
    -o=pipes/p2s-0 \
    -a=dqn \
    --clip=0.25 \
    --ddqn=true \
    --batch-size=32 \
    --lr=0.001 \
    --device=cpu \
    --keepalive