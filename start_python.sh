#! /bin/bash

python src/python/main.py \
    -c=pipe \
    -i=pipes/s2p-0 \
    -o=pipes/p2s-0 \
    -a=dqn \
    --clip=2 \
    --ddqn=true \
    --batch-size=32 \
    --lr=0.0005 \
    --device=cuda:6 \
    --memory-size=50_000 \
    --save-to=saved_dqn \
    --keepalive