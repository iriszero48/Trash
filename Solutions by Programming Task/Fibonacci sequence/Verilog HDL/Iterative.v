module Iterative(
	input clk,
	output [7:0] value
);
reg [7:0] x, previous, current, counter;

assign value = previous;

initial begin
	x <= 8'd10;
	previous <= 8'd0;
	current <= 8'd1;
	counter <= 8'd0;
end

always @(posedge clk) begin
	if (counter < x) begin
		counter <= counter + 8'd1;
		current <= current + previous;
		previous <= current;
	end
end
endmodule
