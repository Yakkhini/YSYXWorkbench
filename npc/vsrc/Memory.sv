module Memory (
    input [1:0] Mwen,
    input [1:0] Mren,
    input [31:0] waddr,
    input [31:0] raddr,
    input [31:0] wdata,
    output bit [31:0] rdata
);

  import "DPI-C" function int vaddr_read(
    int addr,
    bit [1:0] len
  );
  import "DPI-C" function void vaddr_write(
    int addr,
    bit [1:0] len,
    int data
  );

  always_comb begin
    rdata = 0;
    if (Mren[1] | Mren[0]) begin
      rdata = vaddr_read(raddr, Mren);
    end
    if (Mwen[1] | Mwen[0]) begin
      vaddr_write(waddr, Mwen, wdata);
    end
  end

endmodule
