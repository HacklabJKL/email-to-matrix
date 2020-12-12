matrix () {
    payload=${3+--data-binary @-}
    curl -X $1 $payload \
	 -H "Content-Type: ${3-}" \
	 -H "Authorization: Bearer $token" \
         "$server/_matrix/$2"
}

get_id () {
    room_esc=`echo "$1" | jq -Rr @uri`
    matrix POST "client/r0/join/$room_esc" | jq -cr '.room_id | @uri'
}
