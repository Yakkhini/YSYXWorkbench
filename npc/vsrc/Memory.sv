module Memory (
    input rst,
    input clk,
    input [1:0] Mwen,
    input [1:0] Mren,
    input UnsignArithen,
    input [31:0] waddr,
    input [31:0] raddr,
    input [31:0] wdata,
    output bit [31:0] mrdata
);

  // synopsys translate_off
  import "DPI-C" function int vaddr_read(
    int addr,
    bit [1:0] len
  );
  import "DPI-C" function void vaddr_write(
    int addr,
    bit [1:0] len,
    int data
  );

  import "DPI-C" function void mtrace_reset();

  always_comb begin // Maybe cause unexpected behavior
    if ((Mwen[1] | Mwen[0]) & !rst) begin
      vaddr_write(waddr, Mwen, wdata);
    end
  end

  always_comb begin
    rdata_unsign = 0;
    if ((Mren[1] | Mren[0]) & !rst) begin
      rdata_unsign = vaddr_read(raddr, Mren);
    end else begin
      mtrace_reset();
    end
  end
  // synopsys translate_on

  bit [31:0] rdata_unsign;

  MuxKey #(4, 2, 32) mdata_mux (
      .out(mrdata),
      .key(Mren),
      .lut({
        2'b00,
        rdata_unsign,
        2'b01,
        {{24{rdata_unsign[7] & !UnsignArithen & !rst}}, rdata_unsign[7:0]},
        2'b10,
        {{16{rdata_unsign[15] & !UnsignArithen & !rst}}, rdata_unsign[15:0]},
        2'b11,
        rdata_unsign
      })
  );

endmodule
