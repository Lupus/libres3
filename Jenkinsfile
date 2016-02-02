// Generic helper functions
def apt = { packages ->
    ["sudo apt-get update -qq", "sudo apt-get install -y " + packages.join(' '), "sudo apt-get clean"] }

def yum = { packages ->
    ["sudo yum install -y " + packages.join(' '), "sudo yum clean all"] }

def opamcommands = { project -> [
        // "opam update",
        "opam pin add -n ${project} -y .",
        "opam depext ${project}",
        "opam install -y --deps-only ${project}"
]}

def buildIn(base, project, commands) {
    node('docker') {
        stage 'Checkout'
        // Checkout the branch/commit that triggered the build
        checkout scm
        sh 'rm -rf docker.tmp && mkdir docker.tmp && git archive --format=tgz -1 HEAD >docker.tmp/src.tgz'
        sh 'cp -a opam/ docker.tmp/'

        stage 'Build docker image'
        writeFile file:'docker.tmp/Dockerfile', text:[
            "FROM ${base}",
            "EXPOSE 80 443 8008 8443",
            "ENV OPAMJOBS 8",
            "COPY opam/ .",
            "RUN ${commands.join(' && ')}",
            "ADD src.tgz .",
            "RUN OPAMBUILDTEST=true opam pin add ${project} -y ."
        ].join('\n')

        img = docker.build("build-${project}-${base}", 'docker.tmp')
    }
}

def rpm_extra_packages = [
    'sqlite-devel'
]

parallel([
        'centos': { buildIn('ocaml/opam:centos', 'libres3', yum(rpm_extra_packages) + (opamcommands('libres3'))) },
        'debian': { buildIn('ocaml/opam:debian', 'libres3', apt([]) + opamcommands('libres3')) }
])
