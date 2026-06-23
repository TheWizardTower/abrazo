# jq wrapper
def "jq" [...rest: string] {
    /usr/bin/jq ...$rest
}
