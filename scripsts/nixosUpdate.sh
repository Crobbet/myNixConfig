cd /home/bt/myNixConfig || exit

function rebuild-switch()
{

  git add .
  git commit -m "Default"
  git push origin main

  nixos-rebuild switch --flake /home/bt/myNixConfig#Winter

}

echo -e "Please Choose an action \n 1.Rebuild Switch"
read -r Action

case "$Action" in
  1) rebuild-switch
  ;;
  *) exit
  ;;
esac
