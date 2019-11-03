<!DOCTYPE html>
<html>
	<body>  
		<script>
		    Array.prototype.BubbleSort = function () {
                var count = this.length - 1;
                for (var swapped = true; swapped; --count) {
                    swapped = false;
                    for (var i = 0; i < count; i++) {
                        if (this[i] > this[i + 1]) {
                            var tmp = this[i + 1];
                            this[i + 1] = this[i];
                            this[i] = tmp;
                            swapped = true;
                        }
                    }
                }
                return this;
            }
            var input = prompt('input :').split(' ').map(function(x){
                return parseInt(x, 10);
            })
            input.BubbleSort();
            alert(input);
  		</script>
	</body>
</html>
