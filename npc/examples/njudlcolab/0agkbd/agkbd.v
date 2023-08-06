module agkbd(
  input clk,
  input ps2_clk,
  input ps2_data
);

  reg [9:0] buffer;
  reg [3:0] count;
  reg [2:0] ps2_clk_sync;

  always @(posedge clk) begin
        ps2_clk_sync <=  {ps2_clk_sync[1:0],ps2_clk};
  end

  wire sampling = ps2_clk_sync[2] & ~ps2_clk_sync[1];
  always @(posedge clk) begin
    if(sampling) begin
      if (count == 4'd10) begin
        if ((buffer[0] == 0) &&
            (ps2_data) &&
            (^buffer[9:1])) begin      // odd  parity
            $display("receive %x", buffer[8:1]);
        end
        count <= 0;
      end else begin
        buffer[count] <= ps2_data;
        count <= count + 3'd1;
      end
    end
  end
  
endmodule
