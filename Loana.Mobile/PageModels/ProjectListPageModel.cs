using Loana.Mobile.Models;

namespace Loana.Mobile.PageModels
{
    public partial class ProjectListPageModel(ProjectRepository projectRepository) : ObservableObject
    {
        private readonly ProjectRepository _projectRepository = projectRepository;

        [ObservableProperty]
        private List<Project> _projects = [];

        [ObservableProperty]
        private Project? selectedProject;

        [RelayCommand]
        private async Task Appearing()
        {
            Projects = await _projectRepository.ListAsync();
        }

        [RelayCommand]
        private Task? NavigateToProject(Project project)
            => project is null ? Task.CompletedTask : Shell.Current.GoToAsync($"project?id={project.ID}");

        [RelayCommand]
        private async Task AddProject()
        {
            await Shell.Current.GoToAsync($"project");
        }
    }
}