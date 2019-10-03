SERVER="kadenamint-server"
SESSION="kadenamint-session"
TMUX="tmux -L $SERVER"

$TMUX kill-session -t $SESSION
$TMUX new -d -s $SESSION
$TMUX detach

# 0 1
# 2 3
#  4

$TMUX split-window -p 20
$TMUX select-pane -t top
$TMUX split-window -p 50
$TMUX split-window -h -p 50
$TMUX select-pane -t top
$TMUX split-window -h -p 50

$TMUX select-pane -t 0
$TMUX send-keys 'sleep 0' ENTER
$TMUX send-keys './repl' ENTER
$TMUX send-keys 'runNodeDir "test/node0"'

$TMUX select-pane -t 1
sleep 1
$TMUX send-keys 'sleep 1' ENTER
$TMUX send-keys './repl' ENTER
$TMUX send-keys 'runNodeDir "test/node1"'

$TMUX select-pane -t 2
sleep 1
$TMUX send-keys 'sleep 2' ENTER
$TMUX send-keys './repl' ENTER
$TMUX send-keys 'runNodeDir "test/node2"'

$TMUX select-pane -t 3
sleep 1
$TMUX send-keys 'sleep 3' ENTER
$TMUX send-keys './repl' ENTER
$TMUX send-keys 'loadNode "test/node0" >>= addNode "test/nodeX" "nodeX" extraNodePorts >>= runNode'

$TMUX select-pane -t 4
sleep 1
$TMUX send-keys 'sleep 4' ENTER
$TMUX send-keys './repl' ENTER
rm -rf test
$TMUX send-keys 'initNetwork "test" 3' ENTER
$TMUX send-keys 'loadNode "test/node0" >>= showBalancesTx'
$TMUX attach

$TMUX select-pane -t 0
