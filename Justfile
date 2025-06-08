
pack:
  7z a -r CS210-Project-Group-3 ./GEMM ./npc-chisel ./abstract-machine ./flake.* -xr\!build -xr\!out -x\!.* -xr\!.git -tzip
