rm PLang
ghc --make -outputdir /tmp Main.hs
if [[ $? == 0 ]] ; then
    mv Main PLang
    echo "Done!"
fi