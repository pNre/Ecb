
#!/bin/sh
rm -rf assets || true

docker build --tag lambda .
docker rm lambda-build || true
docker create --name lambda-build lambda

mkdir assets

rm -rf lib
mkdir lib
docker cp lambda-build:/usr/lib64/libssl.so lib/libssl.so
docker cp lambda-build:/usr/lib64/libssl.so.1.0.2k lib/libssl.so.1.0.2k
docker cp lambda-build:/usr/lib64/libssl.so.10 lib/libssl.so.10
docker cp lambda-build:/usr/lib64/libcrypto.so lib/libcrypto.so
docker cp lambda-build:/usr/lib64/libcrypto.so.1.0.2k lib/libcrypto.so.1.0.2k
docker cp lambda-build:/usr/lib64/libcrypto.so.10 lib/libcrypto.so.10
docker cp lambda-build:/usr/lib64/libc.so lib/libc.so
docker cp lambda-build:/usr/lib64/libc.so.6 lib/libc.so.6
docker cp lambda-build:/usr/lib64/libc-2.26.so lib/libc-2.26.so
zip assets/libs.zip lib/*

docker cp lambda-build:/app/hook.so hook.so

for target in telegram_interface calendar 
do
    docker cp lambda-build:/app/_build/default/$target/$target.exe $target.exe
    echo "#!/bin/sh\nLD_PRELOAD=hook.so ./$target.exe" > bootstrap
    zip assets/$target.zip bootstrap hook.so $target.exe
    rm $target.exe
done
