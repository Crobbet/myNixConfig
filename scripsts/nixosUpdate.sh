cd /home/bt/myNixConfig || exit

function dellete-generations() {
    nix-collect-garbage -d
}

function rebuild-switch() {

    git add .
    git commit -m "Default"
    git push origin main

    nixos-rebuild switch --flake /home/bt/myNixConfig#Winter

}

echo -e "Please Choose an action \n 1.Rebuild Switch \n 2.dellete-generations \n"
read -r Action

case "$Action" in
1)
    rebuild-switch
    ;;
2)
    dellete-generations
    ;;
*)
    exit
    ;;
esac
