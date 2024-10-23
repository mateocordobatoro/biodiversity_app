const speciesList = [
    "White Stork",
    "European Bison",
    "Gray Wolf",
    "Red Fox",
    "Brown Bear",
    "Common Crane",
    "Lynx",
    "European Beaver",
    "Wild Boar",
    "Moose"
];

document.getElementById('search-button').addEventListener('click', searchSpecies);
document.getElementById('search-input').addEventListener('keypress', function(event) {
    if (event.key === 'Enter') {
        searchSpecies();
    }
});

function searchSpecies() {
    const searchValue = document.getElementById('search-input').value;
    alert(`Searching for: ${searchValue}`);
    // You can replace this with actual search functionality
}

function showMatches(input) {
    const matchList = document.getElementById('match-list');
    matchList.innerHTML = ''; // Clear previous matches
    if (input.length === 0) {
        matchList.style.display = 'none'; // Hide if input is empty
        return;
    }

    const matches = speciesList.filter(species => species.toLowerCase().includes(input.toLowerCase()));
    matches.slice(0, 5).forEach(match => {
        const li = document.createElement('li');
        li.textContent = match;
        li.addEventListener('click', function() {
            document.getElementById('search-input').value = match;
            matchList.style.display = 'none';
        });
        matchList.appendChild(li);
    });
    matchList.style.display = 'block'; // Show matches
}
