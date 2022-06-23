let comment_field = document.getElementById("new_comment_field");

comment_field.value = "@mender-test-bot start pipeline";

console.log(comment_field);

let comment_button = document.getElementsByClassName("btn-primary btn")[2];

// Set the button to enabled
comment_button.disabled = false;
