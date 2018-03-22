# NOTE for [Using the Graph API](https://developers.facebook.com/docs/graph-api/using-graph-api?locale=zh_HK)
```json
GET https://graph.facebook.com/v2.11
  /820882001277849
    ?fields=about,fan_count,website
```
>  - Nodes
`/820882001277849`: node
>> A node is an individual object with a unique ID.
>- Fields
>>Fields are node properties. When you query a node it will return a set of fields by default.
>>`?fields=about,fan_count,website`
>>
>>You can use the fields parameter to specify which fields you want returned for each of the objects returned in the collection.

- Limiting Results

>Limiting allows you to control the number of objects returned in each set of paginated results. To limit results, add a `.limit()` argument to any field or edge.
>
>For example, performing a GET request on the Coca-Cola Page's /feed edge may return hundreds of Posts. You can limit the number of Posts returned for each page of results by doing this:
>
```json
GET https://graph.facebook.com/v2.11
  /820882001277849
    ?fields=feed.limit(3)
```
>
>Only three objects appear in this page of paginated results, but the response included a next field and URL which you can use to fetch the next page.

- Ordering Results

- Publishing

>Most edges allow you to publish objects to a collection on a node. You can do this by using a POST request on the node's edge. For example, you can publish a Comment on a Photo by using the Photo node's /comments edge:
>
```json
POST https://graph.facebook.com
  /1809938745705498
    /comments
      ?message=Awesome!
```

- Traversing Paged Results

>When you make an API request to a node or edge, you usually don't receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated (分頁) by default.

- Cursor-based Pagination

>**Cursor-based** pagination is the most efficient method of paging and should always be used where possible. A cursor refers to a random string of characters which marks a specific item in a list of data. Unless this item is deleted, the cursor will always point to the same part of the list, but will be invalidated if an item is removed. Therefore, your app shouldn't store cursors and assume that they will be valid in the future.
>
>When reading an edge that supports cursor pagination, you will see the following JSON response:
>
```json
{
  "data": [
     ... Endpoint data is here
  ],
  "paging": {
    "cursors": {
      "after": "MTAxNTExOTQ1MjAwNzI5NDE=",
      "before": "NDMyNzQyODI3OTQw"
    },
    "previous": "https://graph.facebook.com/me/albums?limit=25&before=NDMyNzQyODI3OTQw"
    "next": "https://graph.facebook.com/me/albums?limit=25&after=MTAxNTExOTQ1MjAwNzI5NDE="
  }
}
```
>
>
>    - before : This is the cursor that points to the start of the page of data that has been returned.
>    - after : This is the cursor that points to the end of the page of data that has been returned.
>    - next : The Graph API endpoint that will return the next page of data. If not included, this is the last page of data. Due to how pagination works with visibility and privacy, it is possible that a page may be empty but contain a 'next' paging link. Stop paging when the 'next' link no longer appears.
>    - previous : The Graph API endpoint that will return the previous page of data. If not included, this is the first page of data.

