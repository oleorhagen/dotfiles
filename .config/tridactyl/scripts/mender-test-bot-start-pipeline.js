let comment_field = document.getElementById('new_comment_field');

comment_field.value = '@mender-test-bot start pipeline';

console.log(comment_field);

// Steal the focus
comment_field.dispatchEvent(new Event('focus', {
    bubbles: true
}));

// Pretend we've manually entered the text
comment_field.dispatchEvent(new Event('input', {
    bubbles: true
}));

// Unfocus to enable the comment button
comment_field.dispatchEvent(new Event('blur', {
    bubbles: true
}));