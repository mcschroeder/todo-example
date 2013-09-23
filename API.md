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
        "items": [],
        "created_at": "2013-09-23T14:07:20.758Z",
        "updated_at": "2013-09-23T14:07:20.758Z"
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
                "done": true,
                "created_at": "2013-09-23T16:37:22.348Z",
                "updated_at": "2013-09-23T16:45:12.126Z",
            },
            {
                "item_id": 7482473680396593506,
                "text": "Buy cheese",
                "done": false
                "created_at": "2013-09-23T16:38:15.749Z",
                "updated_at": "2013-09-23T16:38:15.749Z",
            }
        ],
        "created_at": "2013-09-23T14:07:20.758Z",
        "updated_at": "2013-09-23T16:45:12.126Z"
    }


## Delete a list

    DELETE /lists/:list_id

### Response

    Status: 204 No Content


# Items

## Create an item

    POST /list/:list_id/items/:item_id

    {
        "text": "Make macaroni & cheese",
    }

### Response

    Status: 201 Created
    Location: /lists/2803027034499563436/items/6505334617479285140

    {
        "item_id": 6505334617479285140,
        "text": "Make macaroni & cheese",
        "done": false,
        "created_at": "2013-09-23T16:40:00.001Z",
        "updated_at": "2013-09-23T16:40:00.001Z"
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
        "done": true,
        "created_at": "2013-09-23T16:38:15.749Z",
        "updated_at": "2013-09-23T16:41:01.304Z",        
    }


## Delete an item

    DELETE /list/:list_id/items/:item_id

### Response

    Status: 204 No Content
