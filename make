rm Viqus
echo "Compiling..."
ghc --make -outputdir /tmp Main.hs | sed -e "s/^/    /"
if [[ $? == 0 ]] ; then
    mv Main Viqus
    echo "Done!"
fi