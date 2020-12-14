function sideEffect() {
    console.log("THIS IS A WARNING OF THINGS TO COME");
}

const dangerous = false;
if (dangerous)
    sideEffect();
else
    console.log("everything is fine");

