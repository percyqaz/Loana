using Loana.Mobile.Models;
using System.Text.Json.Serialization;

[JsonSerializable(typeof(Category))]
[JsonSerializable(typeof(Tag))]
public partial class JsonContext : JsonSerializerContext
{
}