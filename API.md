# Overview

| Methods                | Resource                       |
|------------------------|--------------------------------|
| POST                   | /lists                         |
| GET, DELETE            | /lists/:list_id                |
| POST                   | /lists/:list_id/items          |
| GET, PATCH, DELETE     | /lists/:list_id/items/:item_id |

# Lists

## Create a list

    POST /lists

### Response

    Status: 201 Created
    Location: /lists/2803027034499563436

    {
        "list_id": 2803027034499563436,
        "items": []
    }


## Get a list

    GET /lists/:list_id

### Response

    Status: 200 OK

    {
        "list_id": 2803027034499563436,
        "items": [
            {
                "item_id": 4823141349097802214,
                "text": "Buy macaroni",
                "done": true
            },
            {
                "item_id": 7482473680396593506,
                "text": "Buy cheese",
                "done": false
            }
        ]
    }


## Delete a list

    DELETE /lists/:list_id

### Response

    Status: 204 No Content


# Items

## Create an item

    POST /list/:list_id/items/:item_id

    {
        "text": "Make macaroni & cheese"
    }

### Response

    Status: 201 Created
    Location: /lists/2803027034499563436/items/6505334617479285140

    {
        "item_id": 6505334617479285140,
        "text": "Make macaroni & cheese",
        "done": false
    }


## Update an item

    PATCH /list/:list_id/items/:item_id

    {
        "text": "Buy more cheese",
        "done": true
    }

All of the fields are optional.

### Response

    Status: 200 OK

    {
        "item_id": 7482473680396593506,
        "text": "Buy more cheese",
        "done": true
    }


## Delete an item

    DELETE /list/:list_id/items/:item_id

### Response

    Status: 204 No Content
