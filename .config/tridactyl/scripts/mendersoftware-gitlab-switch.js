// WIP
let current_uri = document.location.href;

console.log("Current URL is: ", +current_uri);

let current_url = new URL(current_uri);

// Replace from GitHub <-> GitLab

function getProject(pathname) {
  if (current_url.host.includes("github")) {
    // https://gitlab.com/Northern.tech/Mender/mender-binary-delta/
    const prefix = "Northern.tech/Mender/";
    current_url.pathname = current_url.pathname.slice(prefix.length);
  } else if (current_url.host.includes("github")) {
    const prefix = "mendersoftware";
  }
}

if (current_url.host.includes("github")) {
  // TODO
  let project = "mender-binary-delta";
  current_url.host = "gitlab.com/";
  current_url.pathname = `/Northern.tech/Mender/${project}/`;
  console.log("Current_URL: " + current_url);
} else if (current_url.host.includes("gitlab")) {
  console.log("Reshaping the GitLab URL to GitHub");
  let project = "mender-binary-delta";
  current_url.host = "github.com/";
  current_url.pathname = `/mendersoftware/${project}/`;
  console.log("Current_URL: " + current_url);
} else {
  console.log("Nooooo");
  // Fail horribly
  foo;
}

// .replace(/https?:\/\//,"git@").replace("/",":").replace(/$/,".git")

// Go to the new URL

console.log("New URL: " + current_url.href);

window.location.href = current_url.href;
