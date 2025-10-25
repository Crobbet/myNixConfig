cd /home/bt/nixos || exit

function rebuild-switch()
{

  git add .
  git commit -m "Default"
  git push origin master

  nixos-rebuild switch --flake /home/bt/nixos#Winter

}

echo -e "Please Choose an action \n 1.Rebuild Switch"
read -r Action

case "$Action" in
  1) rebuild-switch
  ;;
  *) exit
  ;;
esac
