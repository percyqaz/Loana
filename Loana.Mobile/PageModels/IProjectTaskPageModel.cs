using CommunityToolkit.Mvvm.Input;
using Loana.Mobile.Models;

namespace Loana.Mobile.PageModels
{
    public interface IProjectTaskPageModel
    {
        IAsyncRelayCommand<ProjectTask> NavigateToTaskCommand { get; }
        bool IsBusy { get; }
    }
}