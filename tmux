tmux kill-session -t kadenamint
tmux new -d -s "kadenamint"
tmux detach

# 0 1
# 2 3
#  4

tmux split-window -p 20
tmux select-pane -t top
tmux split-window -p 50
tmux split-window -h -p 50
tmux select-pane -t top
tmux split-window -h -p 50

tmux select-pane -t 0
tmux send-keys 'sleep 0' ENTER
tmux send-keys './repl' ENTER
tmux send-keys 'loadInitializedNode "test/node0" >>= runNode'

tmux select-pane -t 1
sleep 1
tmux send-keys 'sleep 1' ENTER
tmux send-keys './repl' ENTER
tmux send-keys 'loadInitializedNode "test/node1" >>= runNode'

tmux select-pane -t 2
sleep 1
tmux send-keys 'sleep 2' ENTER
tmux send-keys './repl' ENTER
tmux send-keys 'loadInitializedNode "test/node2" >>= runNode'

tmux select-pane -t 3
sleep 1
tmux send-keys 'sleep 3' ENTER
tmux send-keys './repl' ENTER
tmux send-keys 'loadInitializedNode "test/node0" >>= addNode "test/nodeX" "nodeX" extraNodePorts >>= runNode'

tmux select-pane -t 4
sleep 1
tmux send-keys 'sleep 4' ENTER
tmux send-keys './repl' ENTER
rm -rf test
tmux send-keys 'initNetwork "test" 3' ENTER
tmux send-keys 'loadInitializedNode "test/node0" >>= showBalancesTx'
tmux attach

tmux select-pane -t 0
