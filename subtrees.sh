#!/bin/bash -ue

cmd=echo

while [[ $[$# >= 1] == 1 ]] ; do
    case $1 in
	clone-non-existing)
	    cmd=clone-non-existing
	    shift 1
	    ;;
	save-state)
	    shift 1
	    cmd=save-state
	    ;;
	load-state)
	    shift 1
	    cmd=load-state
	    ;;
	remove)
	    shift 1
	    cmd=remove
	    ;;
	update-one)
	    shift 1
            cmd="update-one $1"
	    shift 1
	    ;;
	*)
	    echo "invalid arg: " $1
            exit -1
    esac
done

clone-non-existing() {
    path=$1
    url=$2
    branch=$3

    if [[ ! -d ${path} ]] ; then
	git subtree add --squash -P ${path} ${url} ${branch}
    fi
}

update-one() {
    mod=$1

    shift
    path=$1
    url=$2
    branch=$3

    if [[ "$mod" == "$path" ]] ; then
	git subtree pull --squash -P ${path} ${url} ${branch}
    fi
}

if [[ "${cmd}" == "save-state" ]] ; then
    set +e
    echo -n > .git/subtree-state
    for hash in `git log --oneline --format="%H"` ; do
	gss=$(git log -n 1 ${hash} --format="%b" | grep ^git-subtree-split: | { read a b; echo ${b}; })
	gsd=$(git log -n 1 ${hash} --format="%b" | grep ^git-subtree-dir: | { read a b;  echo ${b}; })
	if [[ "${gss}${gsd}" != "" ]] ; then
	    if [[ -x ${gsd} ]] ; then
		grep "^${gsd} " .git/subtree-state >/dev/null || { echo ${gsd} ${gss} >> .git/subtree-state; }
	    fi
	fi
    done
    exit
fi

load-state() {
    path=$1
    url=$2

    branch=$(grep "${path} " .git/subtree-state | { read a b;  echo ${b}; } )
    git subtree add --squash -P ${path} ${url} ${branch}
}

remove() {
    path=$1
    git rm -fr ${1}
}

${cmd} modes/company-mode https://github.com/company-mode/company-mode 0.8.11
${cmd} modes/dash https://github.com/magnars/dash.el.git master
${cmd} modes/diff-hl https://github.com/dgutov/diff-hl.git master
${cmd} modes/dmode https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode.git master
${cmd} modes/flycheck https://github.com/flycheck/flycheck.git 0.21
${cmd} modes/flycheck-rust https://github.com/flycheck/flycheck-rust master
${cmd} modes/ghci-ng https://github.com/chrisdone/ghci-ng.git master
${cmd} modes/gitmodes https://github.com/magit/git-modes.git master
${cmd} modes/grep-a-lot https://github.com/ZungBang/emacs-grep-a-lot.git master
${cmd} modes/haskell-mode https://github.com/haskell/haskell-mode.git master
${cmd} modes/helm https://github.com/Peaker/helm.git master
${cmd} modes/helm-git-grep https://github.com/Peaker/helm-git-grep.git master
${cmd} modes/helm-ls-git https://github.com/Peaker/helm-ls-git.git master
${cmd} modes/helm-proc https://github.com/markus1189/helm-proc.git master
${cmd} modes/hindent https://github.com/chrisdone/hindent.git master
${cmd} modes/magit https://github.com/magit/magit.git master
${cmd} modes/perspective https://github.com/nex3/perspective-el.git 1.12
${cmd} modes/projectile https://github.com/bbatsov/projectile.git v0.12.0
${cmd} modes/rust https://github.com/rust-lang/rust-mode master
${cmd} modes/smartparens https://github.com/Fuco1/smartparens.git master
${cmd} modes/smooth-scrolling https://github.com/aspiers/smooth-scrolling.git master
${cmd} modes/wrap-region https://github.com/rejeep/wrap-region.el.git master
