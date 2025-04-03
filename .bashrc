PS1='\[\e[1;32m\]\u@\h \[\e[0;36m\]\w \[\e[1;33m\]$(branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); [[ -n "$branch" ]] && echo " ($branch)")\n\[\e[1;37m\]\$ \[\e[0m\]'

