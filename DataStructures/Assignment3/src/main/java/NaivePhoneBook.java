import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

public class NaivePhoneBook {

    // Keep list of all existing (i.e. not deleted yet) contacts.
    private List<Contact> contacts = new ArrayList<>();
    private List<String> responses = new ArrayList<>();

    private Query readQuery(Scanner scanner) {
        String type = scanner.next();
        int number = scanner.nextInt();
        if (type.equals("add")) {
            String name = scanner.next();
            return new Query(type, name, number);
        } else {
            return new Query(type, number);
        }
    }

    private void processQuery(Query query) {
        switch (query.type) {
            case "add":
                // if we already have contact with such number,
                // we should rewrite contact's name
                boolean wasFound = false;
                for (Contact contact : contacts)
                    if (contact.number == query.number) {
                        contact.name = query.name;
                        wasFound = true;
                        break;
                    }
                // otherwise, just add it
                if (!wasFound)
                    contacts.add(new Contact(query.name, query.number));
                break;
            case "del":
                for (Iterator<Contact> it = contacts.iterator(); it.hasNext(); )
                    if (it.next().number == query.number) {
                        it.remove();
                        break;
                    }
                break;
            default:
                String response = "not found";
                for (Contact contact : contacts)
                    if (contact.number == query.number) {
                        response = contact.name;
                        break;
                    }
                responses.add(response);
                break;
        }
    }

    public List<String> processQueries(Scanner scanner) {
        int queryCount = scanner.nextInt();
        for (int i = 0; i < queryCount; ++i)
            processQuery(readQuery(scanner));
        return responses;
    }

    static class Contact {
        String name;
        int number;

        public Contact(String name, int number) {
            this.name = name;
            this.number = number;
        }
    }

    static class Query {
        String type;
        String name;
        int number;

        public Query(String type, String name, int number) {
            this.type = type;
            this.name = name;
            this.number = number;
        }

        public Query(String type, int number) {
            this.type = type;
            this.number = number;
        }
    }
}
