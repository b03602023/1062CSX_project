# NOTE for [Using the Graph API](https://developers.facebook.com/docs/graph-api/using-graph-api?locale=zh_HK)
`
GET https://graph.facebook.com/v2.11
  /820882001277849
    ?fields=about,fan_count,website
`
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
>>`
GET https://graph.facebook.com/v2.11
  /820882001277849
    ?fields=feed.limit(3)
`
>
>Only three objects appear in this page of paginated results, but the response included a next field and URL which you can use to fetch the next page.

- Ordering Results

- Publishing

>Most edges allow you to publish objects to a collection on a node. You can do this by using a POST request on the node's edge. For example, you can publish a Comment on a Photo by using the Photo node's /comments edge:
>
>`
POST https://graph.facebook.com
  /1809938745705498
    /comments
      ?message=Awesome!
`

- Traversing Paged Results

>When you make an API request to a node or edge, you usually don't receive all of the results of that request in a single response. This is because some responses could contain thousands of objects so most responses are paginated (分頁) by default.