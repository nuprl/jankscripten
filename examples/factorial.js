function factorial(n) {
    var result = 1;
    while (n > 0) {
        result = (result * n) | 0;
        n = (n - 1) | 0;
    }
    return result;
}

function main() {
    var total = 0;
    var counter = 10000000;    
    while (counter > 0) {
        var n = 10;
        var tmp = factorial(n) | 0;
        total = (total + tmp) | 0;
        counter = (counter - 1) | 0;
    }
    return total;
}

const startTime = Date.now();
console.log(main());
const endTime = Date.now();
console.error(`Running time: ${endTime - startTime}ms`);
